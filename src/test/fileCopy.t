-- we wish to write a program that copies bytes from a source file into a target file,
-- closing all open file handles when it is done or encounters an error

-- first, some helper functions for convenience
do: forall a. (() -> a) -> a
do f = f ()

on: forall a b. a -> (a -> b) -> b
on x f = f x

-- UnsafeFileHandle is an unrestricted type
-- `*` is the kind of all types, and capitalization implies the definition of literals/constructors
'UnsafeFileHandle: *
UnsafeFileHandle: Int -> UnsafeFileHandle

derive Dup UnsafeFileHandle
derive Drop UnsafeFileHandle

-- assume we have FFI implementations of these operations
extern unsafeOpen: String -> Maybe UnsafeFileHandle
extern unsafeReadByte: UnsafeFileHandle -> Maybe Byte
extern unsafeWriteByte: Byte -> UnsafeFileHandle -> ()
extern unsafeClose: UnsafeFileHandle -> ()

-- in the style of System-F°, we can use existential types to package the above into a safe, linear API
-- curly braces are used only for record types
'fileMethods (a: *) = {
    read: a -> (Maybe Byte, a),
    write: Byte -> a -> a,
    close: a -> (),
}

-- we represent failures in opening file handles with exceptions
-- `+` is the kind of effects, and effect constructors define what they return when they're called
'Throws: * -> +
Throw: forall a. a -> [Throws a]()

'FileException: *
CouldNotOpenFile: FileException

-- square brackets in types enclose the set of effects to be handled or propagated out of a function call
openFile: String -> [Throws FileException]exists h. (h, fileMethods h)
openFile (path: String) = on (unsafeOpen path) [
    Nothing. Throw CouldNotOpenFile;
    Just fileHandle. ('UnsafeFileHandle, unsafeFileHandle, {
        readByte h = (unsafeReadByte h, h),
        writeByte b h = do [
            unsafeWriteByte b h,
            h
        ],
        close = unsafeClose,
    }): exists h. (h, fileMethods h)
]

-- since it is inconvenient to manually pass a file handle and its associated methods around,
-- we define an effect to be interpreted based on the existential type instead
'File: * -> +
ReadByte: forall h. [File h](Maybe Byte, h)
WriteByte: forall h. Byte -> [File h]()
Close: forall h. [File h]()

-- we now define an interpreter for commands of effect type `File a`
-- this implementation should be safe due to the linearity of `a`
-- `A <- B` interprets the computation `A` with a linear function `B`
-- bracket patterns match against commands, with `k` as an additive record containing two choices:
-- * `return` resumes control flow at the site of the command's invocation
-- * `abort` redirects control flow to `A`
-- a bracketed wildcard pattern matches if another effect `abort`s `A`
-- a wildcard pattern matches the result of `A` if it returns normally
withFile: forall a. (exists h. (h, fileMethods h)) -> (forall h. [File h]a) -> a
withFile ('h, handle, methods) f = f 'h <- [
    Command ReadByte c: [File 'h].
        (maybeByte, handle) = methods.readByte handle,
        withFile ('h, handle, methods) ['_. c.return maybeByte];
    Command (WriteByte byte) c.
        handle = methods.writeByte handle,
        withFile ('h, handle, methods) ['_. c.return ()];
    Command Close c.
        methods.close handle,
        c.return ();
    Return x.
        methods.close handle,
        x;
    Abort k.
        methods.close handle,
        k ();
]

-- let's define an effect for looping to demonstrate the use of `abort`
'Loop: * -> * -> +
Continue: forall l a. [Loop l a]()
Break: forall l a. a -> [Loop l a]()

loop: forall a. (forall l. [Loop l a]()) -> a
loop 'a f = do [
    'L: *, -- a new type is always fresh in its scope
    f 'L <- [
        Command Continue c: [Loop 'L 'a].
            c.abort (),
            loop 'a f;
        Command (Break x) c.
            c.abort (),
            x;
        Return _. loop 'a f;
        Abort k. k ();
    ]
]

-- now we can finally define the actual copy function
-- note that all its internal effects are handled, so it appears to be a pure function
fileCopy: String -> String -> [Throws FileException]()
fileCopy sourcePath targetPath =
    withFile (openFile sourcePath) ['s.
        withFile (openFile targetPath) ['t.
            loop ['l.
                maybeByte = ReadByte 's,
                on maybeByte [
                    Nothing. Break 'l ();
                    Just byte. WriteByte 't byte
                ]
            ]
        ]
    ]