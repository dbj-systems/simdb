
#include "simdb.h"

// #include "../dbj_cli/dbj_lib/dbj_rt.h"
/*
#include <stdint.h>
#include <atomic>
#include <mutex>
#include <vector>
#include <string>
#include <regex>
#include <random>
#include <iostream>
#include <sstream>
#include <thread>
*/
namespace dbj {

/* 
	Transform any string type to std::string
	MIT (c) 2018 by DBJ.ORG
*/
auto to_string = [](const auto & str) 
	-> std::string 
{

	if constexpr( std::is_same< decltype(str) , std::string >::value ) {
		return str;
	}
	else {
		return std::string(std::begin(str), std::end(str));
	}
};


auto test = []()
{
using namespace std;
		// string literals
	dbj::print(
		to_string( "\nasci string"),
		to_string(L"\nwide string"),
		to_string(u"\nu16  string"),
		to_string(U"\nu32  string")
	) ;
	// std string types
	dbj::print(
		to_string( string("\nasci string") ),
		to_string( wstring(L"\nwide string") ),
		to_string( u16string(u"\nu16  string") ),
		to_string( u32string(U"\nu32  string") )
	);
	// std string_view types
	dbj::print(
		to_string(string_view("\nasci string")),
		to_string(wstring_view(L"\nwide string")),
		to_string(u16string_view(u"\nu16  string")),
		to_string(u32string_view(U"\nu32  string"))
	);
	// vectors of chars of course 
	dbj::print(
		to_string(vector<char>{'\n','a','s','c','i',' ','s','t','r','i','n','g'} ),
		to_string(vector<wchar_t>{L'\n', L'w', L'i', L'd', L'e', L' ', L's', L't', L'r', L'i', L'n', L'g'}),
		to_string(vector<char16_t>{u'\n', u'c', u'h', u'r', u'1', u'6', u's', u't', u'r', u'i', u'n', u'g'}),
		to_string(vector<char32_t>{U'\n', U'c', U'h', U'r', U'1', U'6', U's', U't', U'r', U'i', U'n', U'g'})
	);

	return true;
} ;

	namespace {
		const auto run = test();
	}
}


namespace {
	using    u8 = uint8_t;
	using   u32 = uint32_t;
	using   u64 = uint64_t;
	using    i8 = int8_t;
	using   i32 = int32_t;
	using   i64 = int64_t;
	using  au64 = std::atomic<u64>;
	using  au32 = std::atomic<u32>;
}

/*
---------------------------------------------------------------------------------------------
This badly written completely undocumented plate of spageti seems to have the following
interface:

simdb(const char* name, u32 blockSize, u32 blockCount, bool raw_path = false) :

example: simdbj::simdb db("test", 1024, 512 );

??? db.isOwner()

TBC! will create simdb_test in a "temp" folder. 

TBC! db.getKeyStrs() -- seems to return vector of strings of keys

db.get( 
// ??? lf.data(), (u32)lf.length(), (void*)way.data(), (u32)way.length() 
);
db.put(???)
db.len( ??? )

*/
int main(int argc, char* argv[])
{
  //dbj::print("size of simdb on the stack: ", sizeof(simdb));
  simdbj::simdb db("test", 2<<10, 2<<12);

  if (db.isOwner())
  {
	  dbj::print("\n\nThis proc owns the simdb instance (apparently)");
  }

  auto  lf = "lock free";
  db.put("lock free" , "is the way to be");

  auto val = db.get(lf);

  dbj::print("\n\n the Keys");
  auto keys = db.getKeyStrs();
  for (auto key : keys) {
	  dbj::print("\nKey:\t", key.str /*, " : ", db.get(key.str)*/ );
  }

  auto dbs = simdbj::list_databases();
		dbj::print("\n\n db list: ", dbs );

		db.flush();
		db.close();

  return 0;
}
