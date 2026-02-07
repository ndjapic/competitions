program D_Island_Tour;
uses
	math;
const
    maxn = 200 * 1000;
    inf = maxn * maxn * maxn;
var
    n, m, i, j, x1, x2, d: int32;
    s, sl, sc, ans: int64;
    x, c: array [1 .. maxn] of int32;
    l: array [1 .. maxn] of int64;

begin
    readln(n, m);
    read(x[1]);
    s := 0;

    for i := 0 to n do begin
        l[i] := 0;
        c[i] := 0;
    end;

    for j := 2 to m do begin
        read(x[j]);

        x1 := min(x[j-1], x[j]);
        x2 := max(x[j-1], x[j]);
        d := x2 - x1;

        inc(s, d);
        inc(l[x1], d);
        dec(l[x2], d);
        inc(c[x1]);
        dec(c[x2]);
    end;
    readln;

    ans := inf;
    sl := 0;
    sc := 0;

    for i := 1 to n do begin
        inc(sl, l[i]);
        inc(sc, c[i]);
        ans := min(ans, s - sl * 2 + sc * n);
    end;

    writeln(ans);
end.
