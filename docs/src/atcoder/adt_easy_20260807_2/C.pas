program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 100;
var
	n, m, i, j, k, c: int8;
	ans: boolean;
	f: array [1 .. NN, 1 .. NN] of boolean;
	p: array [1 .. NN] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function superior(i, j: int8): boolean;
var
	k: int8;
begin
	result := p[i] >= p[j];

	if result then begin
		k := 1;
		while result and (k <= m) do begin
			if f[i, k] then result := f[j, k];
			inc(k);
		end;
	end;

	if result then begin
		result := p[i] > p[j];
		if not result then begin

			k := 1;
			while not result and (k <= m) do begin
				result := f[j, k] and not f[i, k];
				inc(k);
			end;

		end;
	end;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	ans := false;
	for i := 1 to n do begin
		for k := 1 to m do f[i, k] := false;

		read(p[i], c);
		while c > 0 do begin
			read(k);
			f[i, k] := true;
			dec(c);
		end;
		readln;

		if not ans then
			for j := 1 to i-1 do
				if not ans then
					ans := superior(i, j) or superior(j, i);
	end;

	if ans then
		writeln('Yes')
	else
		writeln('No');
end.
