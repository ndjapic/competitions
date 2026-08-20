program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #Polish #prefix #notation
uses
	classes, sysutils;
var
	n, i, j, k: int32;
	t: string;
	sl: tstringlist;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function eval(): int64;
begin
	if sl[i] = '+' then begin
		inc(i);
		result := eval() + eval();
	end else if sl[i] = '-' then begin
		inc(i);
		result := eval() - eval();
	end else if sl[i] = '*' then begin
		inc(i);
		result := eval() * eval();
	end else if sl[i] = '/' then begin
		inc(i);
		result := eval() div eval();
	end else begin
		result := strtoint(sl[i]);
		inc(i);
	end;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);
	readln(t);

	sl := tstringlist.create;
	sl.delimiter := ' ';
	sl.delimitedtext := t;

	i := 0;
	writeln(eval());

	readln(k);

	if k > 0 then begin
		for j := 1 to k do begin
			read(i);
			dec(i);
			if sl[i] = '+' then
				sl[i] := '-'
			else if sl[i] = '-' then
				sl[i] := '+'
			else if sl[i] = '*' then
				sl[i] := '/'
			else if sl[i] = '/' then
				sl[i] := '*';
		end;
		readln;
	end;

	i := 0;
	writeln(eval());

	sl.free;
end.
