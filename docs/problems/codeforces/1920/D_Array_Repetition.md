# Problem: D_Array_Repetition.pas

```pascal
program D_Array_Repetition;
uses
    math;
const
    maxn = 200 * 1000 + 1;
    maxk = 1000 * 1000 * 1000 * 1000 * 1000 * 1000;
var
    ntc, tci: int16;
    n, q, i, n2: int32;
    b, j: int8;
    k, r: int64;
    a, op: array [0 .. maxn] of int32;
    c, d: array [0 .. maxn] of int64;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, q);

        n2 := 0;
        op[0] := 0;
        c[0] := 0;
        for i := 1 to n+1 do begin
            if i <= n then
                readln(b, a[i])
            else begin
                b := 2;
                a[i] := 0;
            end;

            if b = 2 then begin
                inc(n2);
                op[n2] := i;
                d[n2] := c[n2-1] + i-1-op[n2-1];
                if d[n2] > maxk div (a[i]+1) then
                    c[n2] := d[n2]
                else
                    c[n2] := d[n2] * (a[i]+1);
            end;
        end;

        for i := 1 to q do begin
            readln(k);
            j := 1;
            while c[j] < k do inc(j);
            r := (k-1) mod d[j];
            while r+1 <= c[j-1] do begin
                k := r;
                dec(j);
                r := (k-1) mod d[j];
            end;
            write(a[op[j-1]+r+1]);
            if i < k then write(' ');
        end;
        writeln;

    end;
end.

```
