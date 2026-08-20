program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i, ans: int8;
	s: string;
	log: boolean;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	log := false;
	ans := 0;
	for i := 1 to n do begin
		readln(s);
		case s[4] of
			'i': log := true;
			'o': log := false;
			'v': if not log then inc(ans);
		end;
	end;

	writeln(ans);
end.
