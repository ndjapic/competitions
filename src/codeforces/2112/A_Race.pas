program A_Race;
var
    ntc, tci, a, x, y: int16;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(a, x, y);

		if (a-x)*(a-y) > 0 then
			writeln('YES')
		else
			writeln('NO');

    end;
end.
