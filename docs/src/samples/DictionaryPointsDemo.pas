program DictionaryPointsDemo;

{$mode delphi}

uses
	Generics.Collections,
	Generics.Defaults;

type
	TPoint = record
		X, Y: Int32;
	end;

type
	TPointComparer = class(TEqualityComparer<TPoint>)
	public
		function Equals(constref Left, Right: TPoint): Boolean; override;
		function GetHashCode(constref Value: TPoint): UInt32; override;
	end;

{ TPointComparer }

function TPointComparer.Equals(constref Left, Right: TPoint): Boolean;
begin
	Result := (Left.X = Right.X) and (Left.Y = Right.Y);
end;

function TPointComparer.GetHashCode(constref Value: TPoint): UInt32;
var
	Hash64: UInt64; // Користимо UInt64 ради сигурног битовног померања
begin
	// Наша проверена математика за спајање две 32-битне координате у 64 бита
	Hash64 := (UInt64(Int64(Value.X)) shl 32) or Cardinal(Value.Y);
	
	// Сабијамо 64 бита у 32 бита користећи исправну 'shr' команду
	Result := UInt32(Hash64 xor (Hash64 shr 32));
end;

var
	S: TDictionary<TPoint, Boolean>;
	Tacka: TPoint;
	P1, P2, P3: TPoint;

begin
	P1.X := 10; P1.Y := 20;
	P2.X := 5;	P2.Y := 5;
	P3.X := 10; P3.Y := 20;

	S := TDictionary<TPoint, Boolean>.Create(TPointComparer.Create);
	try
		S.AddOrSetValue(P1, True);
		S.AddOrSetValue(P2, True);
		S.AddOrSetValue(P3, True); 

		writeln('Укупан број уникатних тачака у скупу: ', S.Count);

		writeln(#10'--- Провера постојања тачака ---');

		Tacka.X := 10; Tacka.Y := 20;
		if S.ContainsKey(Tacka) then
			writeln('Тачка (10, 20) ПОСТОЈИ у скупу.')
		else
			writeln('Тачка (10, 20) НЕ ПОСТОЈИ у скупу.');

		writeln(#10'--- Све тачке у скупу ---');
		for Tacka in S.Keys do begin
			writeln('Тачка: X = ', Tacka.X, ', Y = ', Tacka.Y);
		end;

	finally
		S.Free;
	end;

	readln;
end.
