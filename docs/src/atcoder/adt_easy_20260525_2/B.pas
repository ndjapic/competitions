program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 100;
var
	n, m, i, j: int32;
	w, s: string;
	found: boolean;
	ind: array [0 .. NN] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);
	readln(w);
	m := length(w);

	ind[0] := 0;
	i := 1;

	for j := 1 to m do
		if w[j] = ' ' then begin
			ind[i] := j;
			inc(i);
		end;
	ind[i] := m + 1;

	i := 1;
	found := false;
	while (i <= n) and not found do begin
		s := copy(w, ind[i-1] + 1, ind[i] - ind[i-1] - 1);

		if s = 'and' then
			found := true
		else if s = 'not' then
			found := true
		else if s = 'that' then
			found := true
		else if s = 'the' then
			found := true
		else if s = 'you' then
			found := true;

		inc(i);
	end;

	if found then
		writeln('Yes')
	else
		writeln('No');
end.
