program ManacherProject;
{$MODE DELPHI}
{$H+} // Омогућава дуге стрингове (AnsiString) уместо ShortString
uses SysUtils, Math;

function Manacher(s: string): string;
var
	t: string;
	p: array of Integer;
	C, R, i, i_mirror, maxLen, centerIndex, n, tLen: Integer;
begin
	n := Length(s);
	if n = 0 then Exit('');

	// 1. Оптимизована трансформација стринга: O(n)
	// tLen = 1(^) + 2*n(за # и слова) + 1(#) + 1($) = 2n + 3
	tLen := 2 * n + 3;
	SetLength(t, tLen);
	t[1] := '^';
	for i := 1 to n do
	begin
		t[2 * i] := '#';
		t[2 * i + 1] := s[i];
	end;
	t[tLen - 1] := '#';
	t[tLen] := '$';

	// 2. Иницијализација низа полупречника
	SetLength(p, tLen + 1); 
	C := 0;
	R := 0;

	// 3. Главни алгоритам: O(n)
	for i := 2 to tLen - 1 do
	begin
		i_mirror := 2 * C - i;

		if R > i then
			p[i] := Min(R - i, p[i_mirror])
		else
			p[i] := 0;

		// Ширење без провере граница захваљујући ^ и $
		while t[i + 1 + p[i]] = t[i - 1 - p[i]] do
			p[i] := p[i] + 1;

		if i + p[i] > R then
		begin
			C := i;
			R := i + p[i];
		end;
	end;

	// 4. Проналажење најдужег
	maxLen := 0;
	centerIndex := 0;
	for i := 2 to tLen - 1 do
	begin
		if p[i] > maxLen then
		begin
			maxLen := p[i];
			centerIndex := i;
		end;
	end;

	// Израчунавање почетне позиције у оригиналном стрингу s
	// Формула: (Index_u_t - MaxLen_u_s) div 2
	Result := Copy(s, (centerIndex - maxLen) div 2, maxLen);
end;

// Тест пример
var
	s: string;
begin
	s := 'abaaba';
	WriteLn('String: ', s);
	WriteLn('Najduzi palindrom: ', Manacher(s));
	ReadLn;
end.
