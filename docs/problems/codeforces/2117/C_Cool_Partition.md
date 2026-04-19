# Problem: C_Cool_Partition.pas

```pascal
program C_Cool_Partition;
uses
    math;
const
    nn = 200 * 1000;
var
    ntc, tci, n, i, ans, l, r, mx: int32;
    a, top, adj: array [1 .. nn] of int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);

        for i := 1 to n do begin
            read(a[i]);
            top[i] := n+1;
        end;
        readln;

        for i := n downto 1 do begin
            adj[i] := top[a[i]];
            top[a[i]] := i;
        end;

        ans := 0;
        l := 1;
        r := 1;
        mx := r;
        while r <= n do begin
            inc(ans);
            for i := l to r do mx := max(mx, adj[i]);
            l := r+1;
            r := mx;
        end;

        writeln(ans);

    end;
end.

```
