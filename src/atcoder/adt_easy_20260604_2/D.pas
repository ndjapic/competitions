program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i, errors: int8;
	log: boolean;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	log := false;
	errors := 0;
	for i := 1 to n do begin
		readln(s);
		case s[4] of
			'i': log := true;
			'o': log := false;
			'l': ;
			'v': if not log then inc(errors);
		end;
	end;

	writeln(errors);
end.
