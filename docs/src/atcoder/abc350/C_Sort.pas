program C_Sort;
const
    size_a = 200 * 1000;
var
    n, i, j, k: int32;
    a, inv: array [1 .. size_a] of int32;
    op: array [1 .. size_a] of record
        i, j: int32;
    end;

begin
    readln(n);

    for i := 1 to n do begin

        read(a[i]);
        inv[a[i]] := i;

    end;
    readln;

    k := 0;
    for i := 1 to n do begin
        j := inv[i];
        if j > i then begin

            a[j] := a[i];
            inv[a[j]] := j;

            inc(k);
            op[k].i := i;
            op[k].j := j;

        end;
    end;

    writeln(k);
    for i := 1 to k do
        writeln(op[i].i, ' ', op[i].j);
end.
