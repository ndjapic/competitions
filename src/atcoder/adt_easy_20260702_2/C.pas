program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #costom #sort
uses
	Generics.Collections,
	Generics.Defaults, Math;
const
	NN = 1000;
var
	n, i, o, x, y, z: int32;
	a, b: array [1 .. NN] of int32;
	admitted: array [1 .. NN] of boolean;
	p: TList<int32>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function CompareMath(constref l, r: int32): int32;
begin
	Result := CompareValue(a[r], a[l]);
	if Result = 0 then
		Result := CompareValue(l, r);
end;

function CompareEnglish(constref l, r: int32): int32;
begin
	Result := CompareValue(b[r], b[l]);
	if Result = 0 then
		Result := CompareValue(l, r);
end;

function CompareTotal(constref l, r: int32): int32;
begin
	Result := CompareValue(a[r] + b[r], a[l] + b[l]);
	if Result = 0 then
		Result := CompareValue(l, r);
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, x, y, z);

	p := TList<int32>.Create;
	for i := 1 to n do begin
		p.Add(i);
		admitted[i] := false;
	end;

	for i := 1 to n do read(a[i]); readln;
	p.Sort(TComparer<int32>.Construct(CompareMath));
	for o := 0 to n-1 do begin
		i := p[o];
		if (x > 0) and not admitted[i] then begin
			admitted[i] := true;
			dec(x);
		end;
	end;

	for i := 1 to n do read(b[i]); readln;
	p.Sort(TComparer<int32>.Construct(CompareEnglish));
	for o := 0 to n-1 do begin
		i := p[o];
		if (y > 0) and not admitted[i] then begin
			admitted[i] := true;
			dec(y);
		end;
	end;

	p.Sort(TComparer<int32>.Construct(CompareTotal));
	for o := 0 to n-1 do begin
		i := p[o];
		if (z > 0) and not admitted[i] then begin
			admitted[i] := true;
			dec(z);
		end;
	end;

	for i := 1 to n do
		if admitted[i] then writeln(i);

	p.Free;
end.
