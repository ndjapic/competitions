# Problem: D1_Maximum_And_Queries_easy_version.pas

```pascal
program D1_Maximum_And_Queries_easy_version;
uses
    math;
const
    maxnq = 100 * 1000;
var
    ntc, tci: int16;
    n, i, p2, q, k, ans: int32;
    e: int8;
    a: array [1 .. maxnq] of int32;
    s: array [0 .. 19] of int64;

begin
    {readln(ntc);}
    ntc := 1;
    for tci := 1 to ntc do begin

        for e := 0 to 19 do s[e] := 0;

        readln(n, q);

        for i := 1 to n do begin

            read(a[i]);

            p2 := 1;
            for e := 0 to 19 do begin
                if a[i] and p2 = 0 then inc(s[e], p2 - (a[i] and (p2-1)));
                inc(p2, p2);
            end;

        end;
        readln;

        for i := 1 to q do begin

            readln(k);
            ans := 0;
            p2 := 512 * 1024;

            for e := 19 downto 0 do begin
                if k >= s[e] then begin
                    dec(k, s[e]);
                    inc(ans, p2);
                end;
                p2 := p2 div 2;
            end;

            writeln(ans);

        end;

    end;
end.

```
