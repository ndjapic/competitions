# Problem: C_Neo_s_Escape.pas

```pascal
program C_Neo_s_Escape;
const
    nn = 200 * 1000 + 1;
var
    ntc, tci: int16;
    n, i, l, r, ans: int32;
    a: array [0 .. nn] of int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

		readln(n);

        for i := 1 to n do read(a[i]);
        readln;

        a[0] := 0;
        a[n+1] := 0;

        ans := 0;
        l := 1;
        for r := 1 to n do
            if a[r] <> a[r+1] then begin
                if (a[l-1] < a[l]) and (a[r] > a[r+1]) then
                    inc(ans);
                l := r+1;
            end;

        writeln(ans);

    end;
end.

```
