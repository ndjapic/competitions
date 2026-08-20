program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
var
	q, i, volume: int32;
	a: int8;
	playing: boolean;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(q);

	volume := 0;
	playing := false;
	for i := 1 to q do begin
		readln(a);

		case a of
			1: inc(volume);
			2: volume := max(0, volume - 1);
			3: playing := not playing;
		end;

		if playing and (volume >= 3) then
			writeln('Yes')
		else
			writeln('No');
	end;
end.
