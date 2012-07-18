open System
open System.Runtime.InteropServices

let a = 1u : uint32

module Test =
    let x = 3
    
    [<DllImport("kernel32.dll", SetLastError=true)>]
    extern bool private Beep(uint32 dwFreq, uint32 dwDuration);

    let private a = 1
    let b = 1

    
module Gravatar =
    open System
    open System.Text
    open System.Security.Cryptography

    let email2hash (email: string) =
        use md5provider = MD5.Create()
        md5provider.ComputeHash(Encoding.UTF8.GetBytes(email))
        |> Array.fold (fun result (b: byte) -> result + b.ToString("x2")) ""

    let gravatarURI email =
        @"http://www.gravatar.com/avatar/" + (email2hash email) + @"/?d=identicon"