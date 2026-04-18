# Задатак: C_Gellyfish_and_Flaming_Peony.pas

```pascal
program C_Gellyfish_and_Flaming_Peony;
uses
    math;
const
    nn = 5000;
var
    ntc, tci, n, i, g, x, y, l, r, ans: int32;
    a: array [1 .. nn] of int32;
    dist: array [1 .. nn] of int32;
    bfs: array of int32;

function gcd(x, y: int32): int32;
begin
    if y = 0 then
        gcd := x
    else
        gcd := gcd(y, x mod y);
end;

begin
    setlength(bfs, nn);
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);

        g := 0;
        for i := 1 to n do begin
            read(a[i]);
            g := gcd(a[i], g);
        end;
        readln;

        for x := 1 to nn do dist[x] := n;

        ans := 0;
        for i := 1 to n do begin
            a[i] := a[i] div g;
            if a[i] > 1 then inc(ans);
            bfs[i-1] := a[i];
            dist[a[i]] := 0;
        end;

        l := 0;
        r := n;
        if dist[1] > 0 then dec(ans);

        while dist[1] = n do begin
            x := bfs[l];
            inc(l);

            for i := 1 to n do begin
                y := gcd(x, a[i]);
                if dist[y] > dist[x] + 1 then begin
                    dist[y] := dist[x] + 1;
                    if length(bfs) = r then setlength(bfs, 2*r);
                    bfs[r] := y;
                    inc(r);
                end;
            end;
        end;

        writeln(ans + dist[1]);

    end;
end.

```
