# Problem: B_Good_Start.pas

```pascal
program B_Good_Start;
var
    ntc, tci, w, h, a, b, x1, y1, x2, y2, dx, dy: int32;
    p, px, py: boolean;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(w, h, a, b);
        readln(x1, y1, x2, y2);

		dx := abs(x1 - x2);
		dy := abs(y1 - y2);
		px := dx mod a = 0;
		py := dy mod b = 0;

		if dx = 0 then
			p := py
		else if dy = 0 then
			p := px
		else
			p := px or py;

		if p then
			writeln('Yes')
		else
			writeln('No');

    end;
end.

```
