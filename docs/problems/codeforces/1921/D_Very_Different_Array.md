# Problem: D_Very_Different_Array.pas

```pascal
program D_Very_Different_Array;
uses
    math;
const
    maxn = 200 * 1000;
type
    tarr = array [1 .. maxn] of int32;
var
    ntc, tci: int8;
    n, m, i, la, ra, lb, rb: int32;
    dl, dr, ans: int64;
    a, b, merge: tarr;

procedure msort(var arr: tarr; l, r: int32);
var
    m, i, j, k: int32;
begin
    if r-l > 1 then begin

        m := (l+r) div 2;
        msort(arr, l, m);
        msort(arr, m, r);

        j := l;
        k := m;
        for i := l to r-1 do
            if (k = r) or (j < m) and (
                arr[j] <= arr[k]
            ) then begin
                merge[i] := arr[j];
                inc(j);
            end else begin
                merge[i] := arr[k];
                inc(k);
            end;

        for i := l to r-1 do arr[i] := merge[i];

    end;
end;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, m);

        for i := 1 to n do read(a[i]);
        readln;
        msort(a, 1, n+1);

        for i := 1 to m do read(b[i]);
        readln;
        msort(b, 1, m+1);

        ans := 0;
        la := 1;
        ra := n;
        lb := 1;
        rb := m;

        for i := 1 to n do begin

            dl := abs(a[la] - b[lb]);
            dr := abs(a[ra] - b[rb]);

            if dl >= dr then begin
                inc(ans, dl);
                inc(la);
                inc(lb);
            end else begin
                inc(ans, dr);
                dec(ra);
                dec(rb);
            end;

        end;

        writeln(ans);

    end;
end.

```
