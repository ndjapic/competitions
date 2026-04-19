# Problem: G_Chimpanzini_Bananini.pas

```pascal
program G_Chimpanzini_Bananini;
const
    qq = 200 * 1000;
var
    ntc, tci: int16;
    q, i, l, r: int32;
    reversed: boolean;
    s: int8;
    rizziness, total: int64;
    a: array [-qq .. qq] of int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(q);

        rizziness := 0;
        total := 0;
        l := 0;
        r := 0;
        reversed := false;

        for i := 1 to q do begin
            read(s);
            case s of

                1: if reversed then begin
                    a[r] := a[l];
                    dec(rizziness, int64(r-l) * a[l]);
                    inc(rizziness, total);
                    inc(r);
                    inc(l);
                end else begin
                    dec(r);
                    dec(l);
                    a[l] := a[r];
                    dec(rizziness, int64(r-l) * a[r]);
                    inc(rizziness, total);
                end;

                2: begin
                    rizziness := int64(r-l+1) * total - rizziness;
                    reversed := not reversed;
                end;

                3: if reversed then begin
                    dec(l);
                    read(a[l]);
                    inc(total, a[l]);
                    inc(rizziness, int64(r-l) * a[l]);
                end else begin
                    read(a[r]);
                    inc(total, a[r]);
                    inc(rizziness, int64(r-l+1) * a[r]);
                    inc(r);
                end;

            end;
            readln;
            writeln(rizziness);
        end;

    end;
end.

```
