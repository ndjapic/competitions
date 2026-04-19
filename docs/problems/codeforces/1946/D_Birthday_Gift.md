# Problem: D_Birthday_Gift.pas

```pascal
program D_Birthday_Gift;
const
    maxn = 100 * 1000;
var
    ntc, tci: int16;
    n, i, x, k, s: int32;
    ans: boolean;
    a: array [1 .. maxn] of int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, x);

        k := 0;
        s := 0;
        ans := true;

        for i := 1 to n do begin
            read(a[i]);
            s := s xor a[i];
            ans := false;
            if s or x = x then begin
                s := 0;
                inc(k);
                ans := true;
            end;
        end;
        readln;

        if not ans then k := -1;
        writeln(k);

    end;
end.

```
