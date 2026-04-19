# Problem: D_Slimes.pas

```pascal
program D_Slimes;
uses
    math;
const
    maxn = 300 * 1000;
var
    ntc, tci: int16;
    n, i, l, r, m: int32;
    a, t, link: array [1 .. maxn] of int32;
    s: array [0 .. maxn] of int64;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);
        s[0] := 0;

        for i := 1 to n do begin

            read(a[i]);
            s[i] := s[i-1] + a[i];

            if (i = 1) or (a[i] <> a[i-1]) then
                link[i] := i-1
            else
                link[i] := link[i-1];

            if s[i-1] <= a[i] then
                t[i] := n
            else if a[i-1] > a[i] then
                t[i] := 1
            else if link[i-1] < 1 then
                t[i] := n
            else begin

                r := link[i-1] + 1;
                l := 0;
                while r-l > 1 do begin
                    m := (l+r) div 2;
                    if s[i-1] - s[m-1] > a[i] then
                        l := m
                    else
                        r := m;
                end;
                t[i] := i-l;

            end;

        end;
        readln;

        for i := n downto 1 do begin

            if (i = n) or (a[i] <> a[i+1]) then
                link[i] := i+1
            else
                link[i] := link[i+1];

            if s[n] - s[i] <= a[i] then
                (* pass *)
            else if a[i+1] > a[i] then
                t[i] := 1
            else if link[i+1] > n then
                (* pass *)
            else begin

                l := link[i+1] - 1;
                r := n;
                while r-l > 1 do begin
                    m := (l+r) div 2;
                    if s[m] - s[i] > a[i] then
                        r := m
                    else
                        l := m;
                end;
                t[i] := min(t[i], r-i);

            end;

            if t[i] = n then t[i] := -1;

        end;

        for i := 1 to n-1 do write(t[i], ' ');
        writeln(t[n]);

    end;

end.

```
