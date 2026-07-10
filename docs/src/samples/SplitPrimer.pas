program SplitPrimer;
{$MODE DELPHI}
uses
	SysUtils;
var
	Line: string;
	Tokens: TStringArray; // Није потребно декларисати дужину, Split је сам одређује

begin
	if not SeekEof then
	begin
		Readln(Line);
		// Дели стринг, избацује вишак размака и смешта резултат у Tokens
		Tokens := Line.Split([' '], TStringSplitOptions.ExcludeEmpty);
		
		// Сада кроз Tokens пролазите кроз обичан for-in круг (као у Пајтону!)
		// нпр. за конверзију: Број := StrToInt(Tokens[0]);
	end;
end.
