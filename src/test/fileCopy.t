-- UnsafeFileHandle is an unrestricted type
-- note that capitalization implies the definition of constructors
type UnsafeFileHandle: *
UnsafeFileHandle: Int -> UnsafeFileHandle

derive Dup UnsafeFileHandle
derive Drop UnsafeFileHandle

-- assume we have FFI implementations of these operations
extern unsafeOpen: String -> UnsafeFileHandle
extern unsafeReadByte: UnsafeFileHandle -> Maybe Byte
extern unsafeWriteByte: Byte -> UnsafeFileHandle -> ()
extern unsafeClose: UnsafeFileHandle -> ()

-- in the style of System-F°, we can use existential types to package the above into a safe, linear API
type FileMethods a: *
type FileMethods a = {
    read: a -> (Maybe Byte, a),
    write: Byte -> a -> a,
    close: a -> ()
}

openFile: String -> exists a. (a, FileMethods a)
openFile path = do [
    unsafeFileHandle = unsafeOpen path,
    (unsafeFileHandle, {
        readByte h = (unsafeReadByte h, h),
        writeByte b h = [unsafeWriteByte b h, h]
        close = unsafeClose
    })
]

-- since it is inconvenient to manually pass a file handle and its associated methods around,
-- we define an effect to be interpreted based on the existential type instead
-- `+` is the kind of effect types, and an effect is the type of commands
-- similar to its use in the definition of constructors, capitalization implies the definition of commands
type File a: +
ReadByte: forall a. [File a](Maybe Byte, a)
WriteByte: forall a. Byte -> [File a]()
Close: forall a. [File a]()

-- we must now define an interpreter for commands of type `File a`
-- note that this implementation is guaranteed to be safe by the linearity of `a`
-- in the style of Frank, chevrons (`<` and `>`) in patterns capture:
-- 1. the "shallow" continuation (`k`),
-- 2. the set of linear resources in context at invocation of a command (`l`),
-- 3. and the effect itself for pattern matching
withFile: forall a b. (a, FileMethods a) -> <File a>b -> b
withFile (handle, methods) x = x
withFile (handle, methods) <k, l, ReadByte> = do [
    (maybeByte, handle) = methods.readByte handle,
    withFile (handle, methods) (k l maybeByte)
]
withFile (handle, methods) <k, l, WriteByte byte> = do [
    handle = methods.writeByte handle,
    withFile (handle, methods) (k l ())
]
withFile (handle, methods) <k, l, Close> = do [
    methods.close handle,
    k l () -- there is no need to continue interpreting effects of this type since the handle no longer exists
]

-- now, we wish to write a program that copies bytes from a source file into a target file,
-- exiting and closing all open file handles when it reaches the end of the source file.

-- first, we define an effect for early exit
extern unsafeExit: forall a. Int -> a

type Exit: +
Exit: forall a. Int -> [Exit]a

withExit: forall l a. <Exit>a -> [Destroy l]a
withExit x = x
withExit <k, l, Exit exitCode> = do [
    Destroy l,
    unsafeExit exitCode
]

-- `Fix` is a command for implementing recursion, and its effect is `Div` (short for "divergence")
fileCopy: forall a. String -> String -> [Div]a
fileCopy sourcePath targetPath =
    withFile (openFile sourcePath) [*s.
    withFile (openFile targetPath) [*t.
    Fix [recur ().
        maybeByte = ReadByte *s,
        on maybeByte [
            Nothing. Exit 0;
            Just byte. do [
                WriteByte *t byte,
                recur ()
            ]
        ]
    ]]]

-- wait, shit
-- how do we guarantee that `Close` gets called on every file handle?
