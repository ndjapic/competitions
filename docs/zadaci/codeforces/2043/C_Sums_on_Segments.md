# Задатак: C_Sums_on_Segments.pas

```pascal
program C_Sums_on_Segments;
uses
    math;
const
    nn = 200 * 1000 + 1;
var
    ntc, tci: int16;
    n, i, i0, mn, mx, l1, l2, l3, l4, r1, r2, r3, r4, s, c: int32;
    a: array [1 .. nn] of int32;
    ans: array [1 .. 2*nn] of int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);
        i0 := 1;

        for i := 1 to n do begin
            read(a[i]);
            if (a[i] <> -1) and (a[i] <> 1) then i0 := i;
        end;
        readln;

        l1 := 0; l2 := 0;
        r1 := 0; r2 := 0;

        s := 0;
        for i := i0-1 downto 1 do begin
            inc(s, a[i]);
            l1 := min(l1, s);
            r1 := max(r1, s);
        end;

        s := 0;
        for i := i0+1 to n do begin
            inc(s, a[i]);
            l2 := min(l2, s);
            r2 := max(r2, s);
        end;

        l3 := l1 + a[i0] + l2;
        r3 := r1 + a[i0] + r2;

        s := 0;
        mn := 0; mx := 0;
        for i := i0-1 downto 1 do begin
            inc(s, a[i]);
            mn := min(mn, s);
            mx := max(mx, s);
            l1 := min(l1, s-mx);
            r1 := max(r1, s-mn);
        end;

        s := 0;
        mn := 0; mx := 0;
        for i := i0+1 to n do begin
            inc(s, a[i]);
            mn := min(mn, s);
            mx := max(mx, s);
            l2 := min(l2, s-mx);
            r2 := max(r2, s-mn);
        end;

        l4 := min(l1, l2);
        r4 := max(r1, r2);

        if l3 < l4 then begin

            l1 := l3;
            r1 := r3;
            l2 := max(l4, r3+1);
            r2 := r4;

        end else begin

            l1 := l4;
            r1 := r4;
            l2 := max(l3, r4+1);
            r2 := r3;

        end;

        c := 0;

        for i := l1 to r1 do begin
            inc(c);
            ans[c] := i;
        end;

        for i := l2 to r2 do begin
            inc(c);
            ans[c] := i;
        end;

        writeln(c);
        for i := 1 to c-1 do write(ans[i], ' ');
        writeln(ans[c]);

    end;
end.

```
