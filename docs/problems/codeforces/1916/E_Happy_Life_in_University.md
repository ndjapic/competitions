# Problem: E_Happy_Life_in_University.pas

```pascal
program E_Happy_Life_in_University;
uses
    math;
const
    maxn = 300 * 1000;
var
    ntc, tci: int32;
    n, i, v, d: int32;
    ans: int64;
    is_ca: boolean;
    p, a, diff: array [1 .. maxn] of int32;
    seen_a, seen_v: array [1 .. maxn] of boolean;

begin
    p[1] := 0;
    for i := 1 to maxn do seen_a[i] := false;

    readln(ntc);
    for tci := 1 to ntc do begin

		readln(n);

        for i := 2 to n do read(p[i]); readln;

        for i := 1 to n do begin
            read(a[i]);
            diff[i] := 0;
            seen_v[i] := false;
        end;
        readln;

        ans := 1;
        for i := n downto 1 do
            if diff[i] = 0 then begin

                v := i;
                d := 0;
                is_ca := false;
                while v > 0 do begin
                    if not seen_a[a[v]] then inc(d);
                    seen_a[a[v]] := true;
                    if not is_ca and seen_v[v] then begin
                        is_ca := true;
                        ans := max(ans, int64(d) * diff[v]);
                    end;
                    diff[v] := max(d, diff[v]);
                    ans := max(ans, diff[v]);
                    seen_v[v] := true;
                    v := p[v];
                end;

                v := i;
                while v > 0 do begin
                    seen_a[a[v]] := false;
                    v := p[v];
                end;

            end;

        writeln(ans);

    end;
end.

```
