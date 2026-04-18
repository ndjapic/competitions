program B_LCM;
var
    ntc, tci: int16;
    a1, a2, a3: int8;
    x1, x2: int64;
    ans: boolean;

procedure solve(a1, a2, a3: int8; var x1, x2: int64; var ans: boolean);
var
    i: int8;
begin
    ans := (a1 <= a3) and (a3 <= a1 + a2);
    if ans then begin
        if a1 = a3 then begin
            x1 := 1;
            x2 := 1;
            for i := 1 to a1-1 do x1 := x1 * 10;
            for i := 1 to a2-1 do x2 := x2 * 10;
        end else if a1 + a2 = a3 then begin
            x1 := 9;
            x2 := 9;
            for i := 1 to a1-1 do x1 := x1 * 10;
            for i := 1 to a2-1 do x2 := x2 * 10;
            dec(x2);
        end else begin
            solve(a1-1, a2-1, a3-1, x1, x2, ans);
            x1 := x1 * 10;
            x2 := x2 * 10;
        end;
    end;
end;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(a1, a2, a3);

        if a1 > a2 then
            solve(a1, a2, a3, x1, x2, ans)
        else
            solve(a2, a1, a3, x2, x1, ans);

        if ans then begin
            writeln('Yes');
            writeln(x1, ' ', x2);
        end else
            writeln('No');

    end;
end.
