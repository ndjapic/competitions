# Задатак: D.pas

```pascal
program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	q, i, a, volume: int32;
	music: boolean;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	volume := 0;
	music := false;

	readln(q);
	for i := 1 to q do begin
		readln(a);

		case a of
			1: inc(volume);
			2: if volume > 0 then dec(volume);
			3: music := not music;
		end;

		if music and (volume >= 3) then
			writeln('Yes')
		else
			writeln('No');
	end;
end.

```
