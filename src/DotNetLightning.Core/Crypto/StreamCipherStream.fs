namespace DotNetLightning.Crypto

open System.IO
open NSec.Cryptography

type StreamCipherStream(inner: Stream) =
    inherit System.IO.Stream()
    let _inner = inner
    let _streamcipehr = ChaCha20()

    override this.CanSeek = _inner.CanSeek
    override this.CanRead = _inner.CanRead
    override this.CanWrite = _inner.CanWrite

    override this.Length = _inner.Length
    override this.Position
        with get() = _inner.Position
        and set v = _inner.Position <- v

    override this.Seek(offset, origin) =
        failwith ""

    override this.Close() = _inner.Close()

    override this.Write(data: byte[], offset: int, count: int) =
        failwith ""

    override this.Read(data: byte[], offset: int, count: int) =
        failwith ""

    override this.SetLength(l) =
        failwith ""

    override this.Flush() =
        failwith ""