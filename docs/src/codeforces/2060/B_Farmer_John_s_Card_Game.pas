program B_Farmer_John_s_Card_Game;
const
    nn = 2000;
var
    ntc, tci: int16;
    n, m, i, j, card: int16;
    cow, p: array [0 .. nn] of int16;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, m);

        for i := 1 to n do begin
            for j := 1 to m do begin
                read(card);
                cow[card] := i;
            end;
            readln;
        end;

        card := 0;
        while (card < m*n) and (cow[card mod n] = cow[card]) do
            inc(card);

        if card < m*n then
            writeln(-1)
        else begin
            for card := 0 to n-1 do p[card+1] := cow[card];
            for i := 1 to n-1 do write(p[i], ' ');
            writeln(p[n]);
        end;

    end;
end.
