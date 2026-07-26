program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	d: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(d);

	case d[1] of
		'N': d[1] := 'S';
		'E': d[1] := 'W';
		'W': d[1] := 'E';
		'S': d[1] := 'N';
	end;

	if length(d) > 1 then
		case d[2] of
			'N': d[2] := 'S';
			'E': d[2] := 'W';
			'W': d[2] := 'E';
			'S': d[2] := 'N';
		end;

	writeln(d);
end.
