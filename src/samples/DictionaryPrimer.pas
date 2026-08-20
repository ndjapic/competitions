program SafeDictionaryDemo;

{$mode delphi} // Важно за Generics.Collections

uses
	Generics.Collections, SysUtils;

var
	Dict: TDictionary<Int32, Boolean>;
	Key: Int32;
	Value: Boolean;

begin
	Dict := TDictionary<Int32, Boolean>.Create;
	try
		// Попуњавање помоћу безбедне AddOrSetValue методе
		Dict.AddOrSetValue(1, True);
		Dict.AddOrSetValue(2, False);
		Dict.AddOrSetValue(3, True);
		Dict.AddOrSetValue(4, False);

		// БЕЗБЕДНО ЧИТАЊЕ (без KeyNotFoundException)
		// Тражимо кључ који не постоји
		if Dict.TryGetValue(99, Value) then
			Writeln('Кључ 99 постоји и вредност је: ', Value)
		else
			Writeln('Кључ 99 не постоји. (Безбедно пропуштено)');

		// БЕЗБЕДНО БРИСАЊЕ ТОКОМ ИТЕРАЦИЈЕ (без рушења петље)
		// Услов: Бришемо све кључеве који имају вредност 'False'
		for Key in Dict.Keys.ToArray do
		begin
			// Знамо да кључ постоји јер је извучен из ToArray, али TryGetValue је добра пракса
			if Dict.TryGetValue(Key, Value) then
			begin
				if not Value then // Ако је вредност False
				begin
					// Remove враћа Boolean, не баца изузетак чак и ако је кључ већ обрисан
					if Dict.Remove(Key) then
						Writeln('Успешно обрисан кључ: ', Key);
				end;
			end;
		end;

		// ПРИКАЗ ПРЕОСТАЛИХ ЕЛЕМЕНАТА
		Writeln('--- Преостали елементи у речнику ---');
		for Key in Dict.Keys do begin
			Dict.TryGetValue(Key, Value);
			Writeln('Кључ: ', Key, ', Вредност: ', Value);
		end;

		for Key in Dict.Keys.ToArray do begin
			// Безбедно спајање у једну линију: прво бришемо, али САМО ако је вредност у речнику False
			if (not Dict[Key]) and Dict.Remove(Key) then
				Writeln('Успешно обрисан кључ: ', Key);
		end;

	finally
		Dict.Free;
	end;
	Readln;
end.


program DictionaryPrimer;

{$APPTYPE CONSOLE}

uses
	System.SysUtils, Generics.Collections;

var
	Mapa: TDictionary<string, Integer>;
	Key: string;
	Par: TPair<string, Integer>; // Tip za for-in petlju

begin
	// 1. Inicijalizacija
	Mapa := TDictionary<string, Integer>.Create;
	{Mapa.Capacity := 100000;}
	try
		// 2. Dodavanje i ažuriranje (Items[] radi oba bezbedno)
		Mapa.AddOrSetValue('Srbija', 10);
		Mapa['Grcka'] := 5;
		Mapa['Srbija'] := 15; // Ažuriranje postojeće vrednosti

		// 3. Provera postojanja i čitanje (TryGetValue je najbrži)
		if Mapa.ContainsKey('Srbija') then
			WriteLn('Srbija postoji sa vrednošću: ', Mapa['Srbija']);

		// 4. Prolaz kroz sve elemente (Iteracija)
		WriteLn('--- Sadržaj rečnika ---');
		for Par in Mapa do
			WriteLn(Par.Key, ': ', Par.Value);

		// 5. Brisanje
		Mapa.Remove('Grcka');

		// 6. Broj elemenata
		WriteLn('Preostalo elemenata: ', Mapa.Count);

		// 7. Pražnjenje
		Mapa.Clear;

	finally
		// 8. Oslobađanje memorije (Obavezno!)
		Mapa.Free;
	end;

	ReadLn;
end.


uses
	System.Generics.Collections, System.Generics.Defaults;

var
	KeysList: TList<string>;
	Key: string;
begin
	// Prebacivanje ključeva u listu
	KeysList := TList<string>.Create;
	for Key in Mapa.Keys do
		KeysList.Add(Key);

	// Sortiranje (podrazumevano rastuće)
	KeysList.Sort;

	// Ispis po redosledu
	for Key in KeysList do
		WriteLn(Key, ': ', Mapa[Key]);

	KeysList.Free;
end;


type
	TSafeHash = class(TInterfacedObject, IEqualityComparer<Integer>)
		function Equals(const Left, Right: Integer): Boolean;
		function GetHashCode(const Value: Integer): Integer;
	end;

function TSafeHash.Equals(const Left, Right: Integer): Boolean;
begin
	Result := Left = Right;
end;

function TSafeHash.GetHashCode(const Value: Integer): Integer;
begin
	// Dodavanje nasumičnog XOR-a (seed) otežava predviđanje kolizija
	// Seed bi trebalo da bude neka nasumična vrednost dobijena na početku (npr. Random(MaxInt))
	Result := Value xor 123456789; 
end;

// Upotreba:
var
	Mapa: TDictionary<Integer, Integer>;
begin
	Mapa := TDictionary<Integer, Integer>.Create(TSafeHash.Create);
	// ... dalje se koristi normalno ...
end;


program MultimapPrimer;

{$APPTYPE CONSOLE}

uses
	System.SysUtils, Generics.Collections;

type
	TMultimap = TDictionary<string, TList<Integer>>;

var
	Mapa: TMultimap;
	Lista: TList<Integer>;
	Key: string;
	Value: Integer;

procedure AddValue(AMapa: TMultimap; const AKey: string; AValue: Integer);
var
	L: TList<Integer>;
begin
	// Proveravamo da li lista za taj ključ već postoji
	if not AMapa.TryGetValue(AKey, L) then begin
		// Ako ne postoji, kreiramo novu listu i dodajemo je u rečnik
		L := TList<Integer>.Create;
		AMapa.Add(AKey, L);
	end;
	// Dodajemo vrednost u listu (bilo da je nova ili stara)
	L.Add(AValue);
end;

begin
	Mapa := TMultimap.Create;
	{TObjectDictionary<string, TList<Integer>>.Create([doFreeOnRelease])}
	try
		// Dodavanje vrednosti
		AddValue(Mapa, 'A', 10);
		AddValue(Mapa, 'A', 20);
		AddValue(Mapa, 'B', 50);

		// Iteracija kroz multimap
		for Key in Mapa.Keys do begin
			Write(Key, ': ');
			for Value in Mapa[Key] do
				Write(Value, ' ');
			WriteLn;
		end;

{Lista.BinarySearch(vrednost, index)}

	finally
		// KRITIČNO: Prvo moramo obrisati svaku listu pojedinačno, pa onda rečnik
		for Lista in Mapa.Values do
			Lista.Free;
		Mapa.Free;
	end;

	ReadLn;
end.
