program A_Don_t_Try_to_Count;
uses
    math;
var
    ntc, tci: int16;
    n, m, i, j, ans: int8;
    x, s: array [1 .. 100] of char;

function is_subs(): boolean;
var
    i, j: int8;
    found: boolean;
begin
    i := 1;
    found := false;
    while (i-1+m <= n) and not found do begin
        j := 1;
        while (j <= m) and (s[j] = x[i-1+j]) do inc(j);
        found := j > m;
        inc(i);
    end;
    is_subs := found;
end;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, m);
        for i := 1 to n do read(x[i]); readln;
        for j := 1 to m do read(s[j]); readln;

        ans := 0;
        while (n < 50) and not is_subs() do begin
            for i := 1 to n do x[i+n] := x[i];
            inc(n, n);
            inc(ans);
        end;

        if not is_subs() then ans := -1;
        writeln(ans);

    end;
end.
