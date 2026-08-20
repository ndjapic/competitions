program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	classes;
var
	n, i, o: int8;
	s: string;
	sl: tstringlist;
	ch: char;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);
	readln(s);
	sl := tstringlist.create;
	sl.Delimiter := ' ';
	sl.DelimitedText := s;

	for i := 0 to n-1 do begin
		ch := sl[i][1];
		o := ord(ch) - ord('a');
		if ch >= 's' then dec(o);
		if ch >= 'z' then dec(o);
		o := o div 3 + 2;
		ch := chr(ord('0') + o);
		write(ch);
	end;

	writeln;
	sl.free;
end.
