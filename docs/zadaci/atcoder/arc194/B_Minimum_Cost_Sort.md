# Задатак: B_Minimum_Cost_Sort.pas

```pascal
program B_Minimum_Cost_Sort;
const
    nn = 200 * 1000;
var
    n, i, v, l, r, c: int32;
    ans: int64;
    p: array [1 .. nn] of int32;
    st: array [1 .. 2*nn] of int32;

begin
    readln(n);

    for v := 1 to 2*n do st[v] := 0;

    ans := 0;
    for i := 1 to n do begin
        read(p[i]);

        l := n + p[i];
        r := 2*n-1;
        c := 0;
        while l <= r do begin
            if odd(l) then inc(c, st[l]);
            if not odd(r) then inc(c, st[r]);
            l := (l+1) div 2;
            r := (r-1) div 2;
        end;

        inc(ans, int64(i-1 + i-c) * c div 2);

        v := n-1 + p[i];
        st[v] := 1;
        while v > 1 do begin
            v := v div 2;
            st[v] := st[2*v] + st[2*v+1];
        end;
    end;
    readln;

    writeln(ans);
end.

```
