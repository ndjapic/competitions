# Problem: A_Satisfying_Constraints.pas

```pascal
program A_Satisfying_Constraints;
uses
    math;
const
    maxn = 100;
    maxx = 1000 * 1000 * 1000;
var
    ntc, tci: int16;
    n, i, a: int8;
    x, l, r, t, ans: int32;
    s: array [1 .. maxn] of int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);
        t := 0;
        l := 1;
        r := maxx;

        for i := 1 to n do begin
            readln(a, x);
            case a of
                1: l := max(l, x);
                2: r := min(r, x);
                3: begin
                    inc(t);
                    s[t] := x;
                end;
            end;
        end;

        r := max(r, l-1);
        ans := r-l+1;
        for i := 1 to t do
            if (l <= s[i]) and (s[i] <= r) then dec(ans);
        writeln(ans);

    end;
end.

```
