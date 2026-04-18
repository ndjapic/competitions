program D_Reverse_Madness;
uses
    math;
const
    maxn = 200 * 1000 + 1;
var
    ntc, tci: int16;
    n, k, i, q, j, x, a, b, lo, hi: int32;
    p: boolean;
    s: array [1 .. maxn] of char;
    l, r: array [1 .. maxn] of int32;
    rev: array [1 .. maxn] of boolean;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, k);

        for i := 1 to n do read(s[i]); readln;
        for i := 1 to k do read(l[i]); readln;
        for i := 1 to k do read(r[i]); readln;
        for x := 1 to n do rev[x] := false;

        readln(q);
        for j := 1 to q do begin

            read(x);

            lo := 1;
            hi := k+1;
            while hi - lo > 1 do begin

                i := (lo + hi) div 2;
                if x < l[i] then
                    hi := i
                else
                    lo := i;

            end;
            i := lo;

            a := min(x, r[i] + l[i] - x);
            b := max(x, r[i] + l[i] - x) + 1;
            rev[a] := not rev[a];
            rev[b] := not rev[b];

        end;
        readln;

        p := false;
        for i := 1 to k do
            for x := l[i] to r[i] do begin

                if rev[x] then p := not p;
                if p then
                    write(s[r[i] + l[i] - x])
                else
                    write(s[x]);

            end;
        writeln;

    end;
end.

