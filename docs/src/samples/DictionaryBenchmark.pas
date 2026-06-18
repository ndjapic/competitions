program DictionaryBenchmark;

{$mode delphi}

uses
	Generics.Collections,
	Generics.Defaults,
	SysUtils; // Потребно због GetTickCount64 за мерење времена

type
	TPoint = record
		X, Y: Int32;
	end;
	TPointComparer = class(TEqualityComparer<TPoint>)
	public
		function Equals(constref Left, Right: TPoint): Boolean; override;
		function GetHashCode(constref Value: TPoint): UInt32; override;
	end;

function TPointComparer.Equals(constref Left, Right: TPoint): Boolean;
begin
	Result := (Left.X = Right.X) and (Left.Y = Right.Y);
end;

function TPointComparer.GetHashCode(constref Value: TPoint): UInt32;
var
	Hash64: UInt64;
begin
	Hash64 := (UInt64(Int64(Value.X)) shl 32) or Cardinal(Value.Y);
	Result := UInt32(Hash64 xor (Hash64 shr 32));
end;

const
	BROJ_TACAKA = 1000000; // 1 милион

var
	S: TDictionary<TPoint, Boolean>;
	Tacka: TPoint;
	I: Int32;
	VremePocetka, VremeKraja: Int64;
	PronadjenoCount: Int32;

begin
	writeln('--- Тест перформанси: ТDictionary са 1.000.000 тачака ---'#10);

	// 1. Иницијализација речника
	S := TDictionary<TPoint, Boolean>.Create(TPointComparer.Create);
	
	// ОПТИМИЗАЦИЈА ЗА ТАКМИЧЕЊА: Унапред алоцирамо меморију за милион елемената
	// Ово спречава да речник троши време на стално проширивање табеле у меморији
	S.Capacity := BROJ_TACAKA;

	// 2. МЕРЕЊЕ ВРЕМЕНА УБАЦИВАЊА
	VremePocetka := GetTickCount64;
	
	for I := 1 to BROJ_TACAKA do begin
		// Правимо уникатне тачке (нпр. комбинације итератора)
		Tacka.X := I;
		Tacka.Y := I * 2;
		S.AddOrSetValue(Tacka, True);
	end;
	
	VremeKraja := GetTickCount64;
	writeln('Време убацивања милион тачака: ', VremeKraja - VremePocetka, ' ms');
	writeln('Тренутни број елемената у речнику: ', S.Count);

	// 3. МЕРЕЊЕ ВРЕМЕНА ПРЕТРАГЕ
	PronadjenoCount := 0;
	VremePocetka := GetTickCount64;
	
	// Тражимо милион тачака (неке постоје, неке не постоје)
	for I := 1 to BROJ_TACAKA do begin
		Tacka.X := I;
		// Свака друга тачка у тесту претраге ће заправо постојати у речнику
		if I mod 2 = 0 then
			Tacka.Y := I * 2
		else
			Tacka.Y := 999999; // Ова не постоји

		if S.ContainsKey(Tacka) then
			Inc(PronadjenoCount);
	end;
	
	VremeKraja := GetTickCount64;
	writeln(#10'Време претраге милион тачака: ', VremeKraja - VremePocetka, ' ms');
	writeln('Број успешно пронађених тачака: ', PronadjenoCount);

	// Чишћење
	S.Free;
	
	writeln(#10'Притисните Enter за крај...');
	readln;
end.
