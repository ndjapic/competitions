# Problem: C_K_bonacci.pas

```pascal
program C_K_bonacci;
const
    nn = 1000 * 1000 + 1;
    modulo = 1000 * 1000 * 1000;
var
    n, i, k: int32;
    a: array [0 .. nn] of int32;
    s: array [0 .. nn] of int32;

begin
    readln(n, k);

    s[0] := 0;
    for i := 0 to n do begin
        if i < k then begin
            a[i] := 1;
        end else begin
            a[i] := s[i] - s[i-k];
            if a[i] < 0 then inc(a[i], modulo);
        end;
        s[i+1] := s[i] + a[i];
        if s[i+1] >= modulo then dec(s[i+1], modulo);
    end;

    writeln(a[n]);
end.

```
