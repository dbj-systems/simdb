
#include "simdb.h"

using ::dbj::console::print;

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
  //print("size of simdb on the stack: ", sizeof(simdb));
  simdbj::simdb db("test", 2<<10, 2<<12);

  if (db.isOwner())
  {
	  print("\n\nThis proc owns the simdb instance (apparently)");
  }

  auto  lf = "lock free";
  db.put("lock free" , "is the way to be");

  auto val = db.get(lf);

  print("\n\n the Keys");
  auto keys = db.getKeyStrs();
  for (auto key : keys) {
	  print("\nKey:\t", key.str /*, " : ", db.get(key.str)*/ );
  }

  auto dbs = simdbj::list_databases();
		print("\n\n db list: ", dbs );

		db.flush();
		db.close();

  return 0;
}
