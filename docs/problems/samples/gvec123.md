# Problem: gvec123.pas

```pascal
Program gvec123;
{$MODE DELPHI}
Uses 
    gvector;
Var 
    Buffer: TVector<longint>;
    i: longint;

Begin
    Buffer := TVector<longint>.Create;

    {Push 5 elements at the end of array}
    For i := 1 To 5 Do
        Buffer.PushBack(i);

    {change 3rd element to 47}
    Buffer[2] := 47;

    {pop last element}
    Buffer.PopBack;

    {print all elements}
    For i := 0 To Buffer.Size - 1 Do
        writeln(Buffer[i]);

    Buffer.Destroy;
End.

```
