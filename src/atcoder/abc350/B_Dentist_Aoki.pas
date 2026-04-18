program B_Dentist_Aoki;
const
    maxn = 1000;
var
    n, q, i, t, ans: int16;
    h: array [1 .. maxn] of boolean;

begin
    readln(n, q);
    for t := 1 to n do h[t] := true;

    ans := n;
    for i := 1 to q do begin

        read(t);
        if h[t] then
            dec(ans)
        else
            inc(ans);
        h[t] := not h[t];

    end;
    readln;

    writeln(ans);
end.
