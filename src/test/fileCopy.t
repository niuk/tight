-- we wish to write a program that copies bytes from a source file into a target file,
-- closing all open file handles when it is done or encounters an error

-- first, we need exceptions
-- `+` is the kind of effect types, and an effect is the type of commands
-- capitalization implies the definition of literals; in this case, commands
type Throws: * -> +
Throw: forall a. a -> [Throws a]()

-- a square bracket pattern in the form `[foo. k]` matches the command with the pattern `foo`,
-- and captures in `k` an additive record with two choices, `k.abort` and `k.return`
-- the former is a callback to give the command's caller a chance to do cleanup, whereas
-- the latter is a continuation for returning a value and resuming the caller's control flow
-- note that `k` has a linear type, so one of the choices _must_ be made
catch: forall a b. [Throws a]b -> (a -> b) -> b
catch x _ = x
catch [Throw x. k] except = do [
    k.abort (),
    except x
]

-- UnsafeFileHandle is an unrestricted type
-- capitalization implies the definition of data constructors
type UnsafeFileHandle: *
UnsafeFileHandle: Int -> UnsafeFileHandle

derive Dup UnsafeFileHandle
derive Drop UnsafeFileHandle

-- assume we have FFI implementations of these operations
extern unsafeOpen: String -> Maybe UnsafeFileHandle
extern unsafeReadByte: UnsafeFileHandle -> Maybe Byte
extern unsafeWriteByte: Byte -> UnsafeFileHandle -> ()
extern unsafeClose: UnsafeFileHandle -> ()

-- in the style of System-F°, we can use existential types to package the above into a safe, linear API
type fileMethods: * -> *
type fileMethods a = {
    read: a -> (Maybe Byte, a),
    write: Byte -> a -> a,
    close: a -> ()
}

type FileException: *
CouldNotOpenFile: FileException

openFile: String -> [Throws FileException]exists a. (a, fileMethods a)
openFile path = do [
    maybeFileHandle = unsafeOpen path,
    on maybeFileHandle [
        Nothing. Throw CouldNotOpenFile;
        Just fileHandle. (unsafeFileHandle, {
            readByte h = (unsafeReadByte h, h),
            writeByte b h = do [
                unsafeWriteByte b h,
                h
            ]
            close = unsafeClose
        })
    ]
]

-- since it is inconvenient to manually pass a file handle and its associated methods around,
-- we define an effect to be interpreted based on the existential type instead
type File: * -> +
ReadByte: forall a. [File a](Maybe Byte, a)
WriteByte: forall a. Byte -> [File a]()
Close: forall a. [File a]()

-- we must now define an interpreter for commands of type `File a`
-- note that this implementation is guaranteed to be safe by the linearity of `a`
-- `|` adds callbacks to the `abort` of every command interpretation that "leaks out"
withFile: forall b. (exists a. (a, fileMethods a)) -> (forall a. [File a]b) -> b
withFile (handle, methods) x = on (x *a | [methods.close handle]) [
    [ReadByte (). k]. do [
        (maybeByte, handle) = methods.readByte handle,
        withFile (handle, methods) [*_, k.return maybeByte]
    ],
    [WriteByte byte. k]. do [
        handle = methods.writeByte handle,
        withFile (handle, methods) [*_, k.return ()]
    ],
    [Close (). k]. do [
        methods.close handle,
        k.return ()
    ];
    x. [
        methods.close handle,
        x
    ]
]

-- let's define an effect for looping
type Loop: * -> +
Continue: forall a. [Loop a]()
Break: forall a. a -> [Loop a]()

loop: forall a. (() -> [Loop a]()) -> a
loop f = on (f ()) [
    _. loop f;
    [Continue. k]. do [
        k.abort(),
        loop f
    ];
    [Break x. k]. do [
        k.abort(),
        x
    ];
]

-- now we can finally define the actual copy function
-- note that all its internal effects are handled, so it appears to be a pure function
fileCopy: String -> String -> [Throws FileException]()
fileCopy sourcePath targetPath =
    withFile (openFile sourcePath) [*s.
        withFile (openFile targetPath) [*t.
            loop [
                maybeByte = ReadByte *s,
                on maybeByte [
                    Nothing. Break ();
                    Just byte. WriteByte *t byte
                ]
            ]
        ]
    ]