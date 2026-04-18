# Задатак: E_Wolf.pas

```pascal
program E_Wolf;
uses
    math;
const
    nn = 200 * 1000;
    inf = 1 shl 30;
var
    ntc, tci: int16;
    n, m, i, j, la, ra, lb, rb, ans: int32;
    a, b: array [1 .. nn] of int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, m);

        for i := 1 to n do read(a[i]); readln;
        for j := 1 to m do read(b[j]); readln;

        j := 1;
        for i := 1 to n do
            if (j <= m) and (a[i] >= b[j]) then inc(j);

        if j <= m then begin

            la := 1;
            ra := n;
            lb := 1;
            rb := m;

            while (la <= ra) and (a[la] < b[1]) do inc(la);
            while (la <= ra) and (a[ra] < b[m]) do dec(ra);

            while (la <= ra) and (lb <= rb) and (a[la] >= b[lb]) do begin
                inc(la);
                inc(lb);
            end;

            while (la <= ra) and (lb <= rb) and (a[ra] >= b[rb]) do begin
                dec(ra);
                dec(rb);
            end;

            if lb = rb then
                writeln(b[lb])
            else begin

                lb := 1;
                rb := m;

                for la := 1 to n do
                    if a[la] >= b[lb] then inc(lb);

                for ra := n downto 1 do
                    if a[ra] >= b[rb] then dec(rb);

                ans := inf;
                if lb = m then ans := min(ans, b[m]);
                if rb = 1 then ans := min(ans, b[1]);

                if ans = inf then ans := -1;
                writeln(ans);
            end;

        end else
            writeln(0);

    end;
end.

```
