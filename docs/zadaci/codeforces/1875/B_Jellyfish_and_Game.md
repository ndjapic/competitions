# Задатак: B_Jellyfish_and_Game.pas

```pascal
program B_Jellyfish_and_Game;
uses
    math;
const
    maxn = 50;
    inf = 1000 * 1000 * 1000;
var
    ntc, tci: int16;
    n, m, i, j: int8;
    k, mna, mxa, mnb, mxb: int32;
    ans: int64;
    a, b: array [1 .. maxn] of int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, m, k);

        mna := inf;
        mxa := 1;
        mnb := inf;
        mxb := 1;
        ans := 0;

        for i := 1 to n do begin
            read(a[i]);
            mna := min(mna, a[i]);
            mxa := max(mxa, a[i]);
            inc(ans, a[i]);
        end;
        readln;

        for j := 1 to m do begin
            read(b[j]);
            mnb := min(mnb, b[j]);
            mxb := max(mxb, b[j]);
        end;
        readln;

        if odd(k) then begin

            if mna < mnb then begin

                if mxa < mnb then
                    inc(ans, mxb-mna)
                else if mxa < mxb then
                    inc(ans, mxb-mna)
                else
                    inc(ans, mxb-mna);

            end else begin

                if mxa < mxb then
                    inc(ans, mxb-mna)
                else if mna < mxb then
                    inc(ans, mxb-mna)
                else
                    inc(ans, 0);

            end;

        end else begin

            if mna < mnb then begin

                if mxa < mnb then
                    inc(ans, 0)
                else if mxa < mxb then
                    inc(ans, 0)
                else
                    inc(ans, mxb-mxa);

            end else begin

                if mxa < mxb then
                    inc(ans, mnb-mna)
                else if mna < mxb then
                    inc(ans, mnb-mna+mxb-mxa)
                else
                    inc(ans, mnb-mxa);

            end;

        end;

        writeln(ans);

    end;
end.


```
