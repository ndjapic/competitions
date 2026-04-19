# Problem: A_Forked.pas

```pascal
program A_Forked;
type
    tcell = record
        x, y: int32;
    end;
    tarr = array [1 .. 8] of tcell;
var
    ntc, tci, a, b: int32;
    k, q: tcell;
    mk, mq: tarr;
    i, j: int8;
    ans: int8;

procedure moves(var m: tarr; f: tcell);
begin
    m[1].x := f.x + a; m[1].y := f.y + b;
    m[2].x := f.x - a; m[2].y := f.y + b;
    m[3].x := f.x - a; m[3].y := f.y - b;
    m[4].x := f.x + a; m[4].y := f.y - b;

    m[5].x := f.x + b; m[5].y := f.y + a;
    m[6].x := f.x - b; m[6].y := f.y + a;
    m[7].x := f.x - b; m[7].y := f.y - a;
    m[8].x := f.x + b; m[8].y := f.y - a;
end;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(a, b);
        readln(k.x, k.y); moves(mk, k);
        readln(q.x, q.y); moves(mq, q);

        ans := 0;
        if a = b then begin
            for i := 1 to 4 do
                for j := 1 to 4 do
                    if (mk[i].x = mq[j].x) and (mk[i].y = mq[j].y) then
                        inc(ans);
        end else begin
            for i := 1 to 8 do
                for j := 1 to 8 do
                    if (mk[i].x = mq[j].x) and (mk[i].y = mq[j].y) then
                        inc(ans);
        end;

        writeln(ans);

    end;
end.

```
