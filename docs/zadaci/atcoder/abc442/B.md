# Задатак: B.pas

```pascal
program B_;
uses
	math;
var
	q, i, a, volume: int32;
	playing: boolean;

begin
	readln(q);
	volume := 0;
	playing := false;

	for i := 1 to q do begin

		readln(a);
		case a of
			1: inc(volume);
			2: volume := max(0, volume-1);
			3: playing := not playing;
		end;

		if volume < 3 then
			writeln('No')
		else if playing then
			writeln('Yes')
		else
			writeln('No');

	end;
end.

```
