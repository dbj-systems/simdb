
#include "simdb.h"

#include "../dbj_cli/dbj_lib/dbj_rt.h"

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

#pragma warning(push, 0)

namespace {
	using    u8 = uint8_t;
	using   u32 = uint32_t;
	using   u64 = uint64_t;
	using    i8 = int8_t;
	using   i32 = int32_t;
	using   i64 = int64_t;
	using  au64 = std::atomic<u64>;
	using  au32 = std::atomic<u32>;

	void printkeys(simdbj::simdb const& db)
	{
		auto keys = db.getKeyStrs();
		for (auto key : keys) {
			dbj::print("\n", key.str, " : ", db.get(key.str));
		}
	}
}

/*
---------------------------------------------------------------------------------------------
*/
int main()
{
  // using namespace std;
  //dbj::print("size of simdb on the stack: ", sizeof(simdb));

	simdbj::simdb db("test", 2<<10, 2<<12);

  std::string numkey[] = {"0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11"};
  std::string  label[] = {"zero","one","two","three","four","five","six","seven","eight","nine","ten","eleven"};
  std::string       wat  =       "wat";
  std::string       wut  =       "wut";
  std::string  skidoosh  =  "skidoosh";
  std::string    kablam  =    "kablam";
  std::string   longkey  =  "this is a super long key as a test";
  std::string   longval  =  "value that is really long as a really long value test";

  std::string  lf = "lock free";
  std::string way = "is the way to be";
  
  i64    len = db.len( lf.data(), (u32)lf.length() );
  std::string way2(len,'\0');
  db.get( lf.data(), (u32)lf.length(), (void*)way.data(), (u32)way.length() );

  dbj::print("\n",way,"\n");

  if( db.isOwner() )
  {
    dbj::print("\nput: ", db.put("lock free", "is the way to be") );
    dbj::print("\nput: ", db.put(wat, skidoosh) );
    //db.del("wat");
    dbj::print("\nput: ", db.put( wut.data(),   (u32)wut.length(),    kablam.data(),   (u32)kablam.length())   ); 
    //db.del("wut");
    dbj::print("\nput: ", db.put(kablam, skidoosh) ); 
	//dbj::print("put: ", db.put( kablam.data(),(u32)kablam.length(), skidoosh.data(), (u32)skidoosh.length()) ); 
    //db.del("kablam");
  
    dbj::print("\nput: ", db.put(wat, skidoosh) );
  }
    dbj::print("\n\n the Keys\n\n");
	printkeys(db);

  auto dbs = simdbj::simdb_listDBs();
		dbj::print("\n\n db list: ", dbs );

		dbj::log.flush();
  return 0;
}

 #pragma warning(pop)


#if 0
namespace {

	//#include <SIM/SIM_GeneralTemplateUtil.hpp>

#ifndef COMBINE
#define COMBINE2(a,b) a ## b
#define COMBINE(a,b) COMBINE2(a,b)
#endif

#ifndef PAUSE
#define PAUSE std::cout << "Paused at line " << __LINE__ << std::endl; int COMBINE(VAR,__LINE__); std::cin >> COMBINE(VAR,__LINE__);
#endif

#ifndef TO
#define TO(to, var) for(std::remove_const<decltype(to)>::type var = 0; var < to; ++var)
	//#define TO(to, var) for(auto var = 0ull; var < (unsigned long long)to; ++var)
#endif

	u32   intHash(u32    h)
	{
		//h += 1;
		h ^= h >> 16;
		h *= 0x85ebca6b;
		h ^= h >> 13;
		h *= 0xc2b2ae35;
		h ^= h >> 16;
		return h;
	}
	u32  nextPowerOf2(u32    v)
	{
		v--;
		v |= v >> 1;
		v |= v >> 2;
		v |= v >> 4;
		v |= v >> 8;
		v |= v >> 16;
		v++;

		return v;
	}

	template <typename T> struct RngInt
	{
		std::mt19937                       m_gen;
		std::uniform_int_distribution<T>   m_dis;

		RngInt(T lo = 0, T hi = 1, int seed = 16807)
			: m_gen(seed), m_dis(lo, hi)
		{ }

		inline T operator()()
		{
			return m_dis(m_gen);
		}

		inline T operator()(T lo, T hi)
		{
			std::uniform_int_distribution<T>  dis(lo, hi);
			return dis(m_gen);
		}
	};

	template<class STR>
	STR keepAlphaNumeric(STR const& s)
	{
		using namespace std;

		regex            alphaNumeric("[a-zA-Z\\d]+");
		sregex_iterator  iter(ALL(s), alphaNumeric);
		sregex_iterator  iter_end;

		STR out;
		while (iter != iter_end)
			out += iter++->str();      // ...

		return out;
	}

	template<class STR1, class STR2>
	STR1 subNonFilename(STR1 const& s, STR2 const& substr)
	{
		using namespace std;

		STR1      patStr(":|\\*|\\.|\\?|\\\\|/|\\||>|<");
		regex     pattern(patStr);
		return regex_replace(s, pattern, substr);
	}

	template<class T>
	inline auto Concat(const T& a) -> T
	{
		return a;
	}
	template<class T1, class... T>
	inline auto
		Concat(const T1& a, const T&... args) -> T1
	{
		return a + Concat(args...);
	}

	inline std::string
		toString(std::vector<std::string> const& v)
	{
		using namespace std;

		ostringstream convert;
		TO(v.size(), i) convert << v[i] << " ";
		convert << endl;
		return convert.str();
	}

	template<class T> inline std::string
		toString(T const& x)
	{
		std::ostringstream convert;
		convert << x;
		return convert.str();
	}

	template<class T1, class... T> inline std::string
		toString(const T1& a, const T&... args)
	{
		return toString(a) + toString(args...);
	}

	inline std::ostream&  Print(std::ostream& o) { return o; }
	template<class... T> inline std::ostream&
		Print(std::ostream& o, const T&... args)
	{
		o << toString(args ...);
		o.flush();
		return o;
	}
	template<class... T> inline std::ostream&
		Println(std::ostream& o, const T&... args)
	{
		//o << toString(args...) << std::endl;
		Print(o, args..., "\n");
		return o;
	}
	template<class... T> inline void
		Print(const T&... args)
	{
		Print(std::cout, args...);
		//std::cout << toString(args...);
	}
	template<class... T> inline void
		Println(const T&... args)
	{
		Println(std::cout, args...);
		//std::cout << toString(args...) << std::endl;
	}
	template<class T> inline void
		PrintSpaceln(const T& a)
	{
		Print(std::cout, a);
	}
	template<class T1, class... T> inline void
		PrintSpaceln(const T1& a, const T&... args)
	{
		Print(std::cout, a, " ");
		PrintSpaceln(args...);
		Println();
	}

	using std::thread;
	using str = std::string;

	template<class T, class A = std::allocator<T> > using vec = std::vector<T, A>;  // will need C++ ifdefs eventually

	void printkey(simdb const& db, str const& key)
	{
		auto val = db.get(key);
		Println(key, ": ", val);
	}

	void printdb(simdb const& db)
	{
		Println("size: ", db.size());

		//str memstr;
		//memstr.resize(db.size()+1);

		vec<i8> memv(db.memsize(), 0);
		memcpy((void*)memv.data(), db.mem(), db.memsize());

		//str memstr( (const char*)db.data(), (const char*)db.data() + db.size());
		//Println("\nmem: ", memstr, "\n" );

		Println("\n");

		u64 blksz = db.blockSize();
		TO(memv.size(), i) {
			if (i % blksz == 0) {
				putc('|', stdout);
			}
			putc(memv[i], stdout);
		}
	}



	void printhsh(simdb const& db)
	{
		u32* d = (u32*)db.hashData();
		for (u32 i = 0; i < (db.blocks() * 2); ++i) {
			if (i % 4 == 0) printf("|");
			else if (i % 2 == 0) printf(" ");

			printf(" 0x%08x ", d[i]);

			//if(i%8) printf("|");
			//else if(i%4) printf(" ");
		}
		printf("\n\n");

	}

} // nspace
#endif