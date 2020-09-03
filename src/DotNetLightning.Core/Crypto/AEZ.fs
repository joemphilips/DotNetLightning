namespace DotNetLightning.Crypto

module private AEZ_Constants =
    [<Literal>]
    let VERSION = "v5"
    
    let EXTRACTED_KEY_SIZE = 16 * 3
    
    [<Literal>]
    let BLOCK_SIZE = 16
    
module private AEZ_AEAD_Constants =
    [<Literal>]
    let AEAD_NONSE_SIZE = 16
    
    [<Literal>]
    let AEAD_OVERHEAD = 16
    
type AEADAEZ = {
    // E
    key: byte[]
}

