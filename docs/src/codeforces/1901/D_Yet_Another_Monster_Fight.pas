program D_Yet_Another_Monster_Fight;
uses
    math;
const
    maxn = 300 * 1000 + 1;
var
    n, i, x: int32;
    a, pre, suf: array [0 .. maxn] of int32;

begin
    readln(n);

    pre[0] := -1;
    for i := 1 to n do begin
        read(a[i]);
        pre[i] := max(pre[i-1]+1, a[i]);
    end;
    readln;

    suf[n+1] := -1;
    for i := n downto 1 do
        suf[i] := max(suf[i+1]+1, a[i]);

    x := high(int32);
    for i := 1 to n do
        x := min(
            x, 
            max(
                max(pre[i], suf[i+1] + i),
                max(suf[i], pre[i-1] + n-i+1)
            )
        );

    writeln(x);
end.
