program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 100;
type
	TCell = record
		i, j: int8;
	end;
var
	n, i, j, e: int8;
	ans: boolean;
	c: TCell;
	a, b: array [1 .. NN, 1 .. NN] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function rotate(i, j, e: int8): TCell;
begin
	if e = 0 then begin
		Result.i := i;
		Result.j := j;
	end else if odd(e) then begin
		Result := rotate(n+1-j, i, e-1);
	end else begin
		e := e div 2;
		Result := rotate(i, j, e);
		Result := rotate(Result.i, Result.j, e);
	end;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	for i := 1 to n do begin
		for j := 1 to n do read(a[i, j]);
		readln;
	end;

	for i := 1 to n do begin
		for j := 1 to n do read(b[i, j]);
		readln;
	end;

	ans := false;
	for e := 101 to 104 do
		if not ans then begin
			ans := true;
			for i := 1 to n do
				if ans then
					for j := 1 to n do
						if ans then begin
							c := rotate(i, j, e);
							if a[c.i, c.j] = 1 then
								ans := b[i, j] = 1;
						end;
		end;

	if ans then
		writeln('Yes')
	else
		writeln('No');
end.
