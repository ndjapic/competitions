program C_Snake_Queue;
const
    qq = 300 * 1000;
var
    q, i, l, r, len, k: int32;
    qt: int8;
    queue: array [0 .. qq] of int64;

begin
    readln(q);

    l := 0;
    r := 0;
    queue[0] := 0;

    for i := 1 to q do begin
        read(qt);
        case qt of

            1: begin
                read(len);
                inc(r);
                queue[r] := queue[r-1] + len;
            end;

            2: inc(l);

            3: begin
                read(k);
                writeln(queue[l+k-1] - queue[l]);
            end;

        end;
        readln;
    end;
end.
