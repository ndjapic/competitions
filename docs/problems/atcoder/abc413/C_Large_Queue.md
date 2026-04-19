# Problem: C_Large_Queue.pas

```pascal
program C_Large_Queue;
uses
    math;
const
    nn = 200 * 1000;
var
    q, i, l, r, k, mn: int32;
    tp: int8;
    ans: int64;
    a: array [1 .. nn] of record
        c, x: int32;
    end;

begin
    readln(q);
    l := 1;
    r := 0;

    for i := 1 to q do begin
        read(tp);
        case tp of

            1: begin
                inc(r);
                readln(a[r].c, a[r].x);
            end;

            2: begin
                readln(k);
                ans := 0;
                while k > 0 do begin
                    mn := min(k, a[l].c);
                    inc(ans, int64(a[l].x) * mn);
                    dec(k, mn);
                    dec(a[l].c, mn);
                    if a[l].c = 0 then inc(l);
                end;
                writeln(ans);
            end;

        end;
    end;
end.

```
