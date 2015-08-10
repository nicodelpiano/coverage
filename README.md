# ecl

The Nats example throws this:

```
Prelude> :l Nat.hs 
[1 of 1] Compiling Nat              ( Nat.hs, interpreted )
Ok, modules loaded: Nat.
*Nat> checkNat [([Zero, Zero], Nothing)]
Loading package ecl-0.1.0.0 ... linking ... done.
([[Succ NullBinder,NullBinder],[NullBinder,Succ NullBinder]],Just True)
*Nat> checkNat [([Zero, Zero], Nothing), ([Zero, Zero], Nothing)]
([[Succ NullBinder,NullBinder],[Succ NullBinder,Succ NullBinder],[NullBinder,Succ NullBinder]],Just False)
```
