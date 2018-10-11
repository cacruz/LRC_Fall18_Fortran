! file dlfcn.f90
module dlfcn
   use iso_c_binding
   use iso_c_utilities
   implicit none
   private

   public :: dlopen, dlsym, dlclose, dlerror ! dl api
   
   ! valid modes for mode in dlopen:
   integer, parameter, public :: rtld_lazy=1, rtld_now=2, rtld_global=256, rtld_local=0
      ! obtained from the output of the previously listed c program 
         
   interface ! all we need is interfaces for the prototypes in <dlfcn.h>
      function dlopen(file,mode) result(handle) bind(c,name="dlopen")
         ! void *dlopen(const char *file, int mode);
         use iso_c_binding
         character(c_char), dimension(*), intent(in) :: file
            ! c strings should be declared as character arrays
         integer(c_int), value :: mode
         type(c_ptr) :: handle
      end function
      function dlsym(handle,name) result(funptr) bind(c,name="dlsym")
         ! void *dlsym(void *handle, const char *name);
         use iso_c_binding
         type(c_ptr), value :: handle
         character(c_char), dimension(*), intent(in) :: name
         type(c_funptr) :: funptr ! a function pointer
      end function
      function dlclose(handle) result(status) bind(c,name="dlclose")
         ! int dlclose(void *handle);
         use iso_c_binding
         type(c_ptr), value :: handle
         integer(c_int) :: status
      end function
      function dlerror() result(error) bind(c,name="dlerror")
         ! char *dlerror(void);
         use iso_c_binding
         type(c_ptr) :: error
      end function         
   end interface
      
end module
