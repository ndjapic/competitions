program D_Pyramid;
uses
    math;
const
    maxn = 200 * 1000 + 1;
var
    n, i, ans: int32;
    a, pre, suf: array [0 .. maxn] of int32;

begin
    readln(n);
    for i := 1 to n do read(a[i]);
    readln;

    pre[0] := 0;
    for i := 1 to n do pre[i] := min(a[i], a[i-1] + 1);

    suf[n+1] := 0;
    for i := n downto 1 do suf[i] := min(a[i], suf[i+1] + 1);

    ans := 0;
    for i := 1 to n do
        ans := max(ans, min(pre[i], suf[i]));
    writeln(ans);
end.
