program B_Card_Game;
{$mode objfpc}{$H+}{$J-}
uses
    math;
var
    ntc, tci: int16;
    a1, a2, b1, b2, pa, pb, ans: int8;

procedure round(var pa, pb: int8; a, b: int8);
begin
	if a > b then
		inc(pa)
	else if b > a then
		inc(pb);
end;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(a1, a2, b1, b2);

		ans := 0;

		pa := 0;
		pb := 0;
		round(pa, pb, a1, b1);
		round(pa, pb, a2, b2);
		if pa > pb then inc(ans, 2);

		pa := 0;
		pb := 0;
		round(pa, pb, a1, b2);
		round(pa, pb, a2, b1);
		if pa > pb then inc(ans, 2);

        writeln(ans);

    end;
end.
