# Задатак: D_Small_GCD.pas

```pascal
program D_Small_GCD;
const
    maxn = 80 * 1000;
    small_num = 8000;
var
    ntc, tci, n, i, j, x, y: int32;
    ans, s: int64;
    a, merge: array [1 .. maxn] of int32;
    gcd_memo: array [0 .. small_num, 0 .. small_num] of int32;

function gcd(a, b: int32): int32;
begin
    if a < b then
        gcd := gcd(b, a)
    else if a <= small_num then
        gcd := gcd_memo[a, b]
    else if not odd(a) then begin
        if not odd(b) then
            gcd := gcd(a shr 1, b shr 1) shl 1
        else
            gcd := gcd(a shr 1, b);
    end else if not odd(b) then
        gcd := gcd(a, b shr 1)
    else if a > b then
        gcd := gcd(a-b, b)
    else
        gcd := gcd(a, b-a);
end;

procedure msort(l, r: int32);
var
    m, i, j, k: int32;
begin
    if l < r then begin

        m := (l+r) div 2;
        msort(l, m);
        msort(m+1, r);

        j := l;
        k := m+1;
        for i := l to r do
            if (k > r) or (j <= m) and (a[j] <= a[k]) then begin
                merge[i] := a[j];
                inc(j);
            end else begin
                merge[i] := a[k];
                inc(k);
            end;

        for i := l to r do a[i] := merge[i];

    end;
end;

begin
    for x := 1 to small_num do begin
        gcd_memo[x, 0] := x;
        for y := 1 to x do
            gcd_memo[x, y] := gcd_memo[y, x mod y];
    end;

    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);
        for i := 1 to n do read(a[i]); readln;
        msort(1, n);

        ans := 0;
        for j := n-1 downto 2 do begin
            s := 0;
            for i := j-1 downto 1 do
                inc(s, gcd(a[j], a[i]));
            inc(ans, s * (n-j));
        end;

        writeln(ans);

    end;
end.

```
