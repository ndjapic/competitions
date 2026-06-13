program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s);

	case s[1] of

		's': case s[6] of
			's': writeln('1');
			'f': writeln('2');
		end;

		'f': case s[6] of
			's': writeln('3');
			'f': writeln('4');
		end;

	end;
end.
