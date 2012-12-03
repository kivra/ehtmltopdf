<<<<<<< HEAD
# ehtmltopdf

Erlang wrapper for converting HTML to PDF.

Right now it wraps `wkhtmltopdf' and it might eventually wrap other external
programs or libraries.

The application environment config should indicate where wkhtmltopdf is
installed by setting the Application configuration parameter `wkhtmltopdf_path`
with a string in your `app.config'. The default assumes is `"wkhtmltopdf"' which
implies that it in your `PATH`.

Example setting:

    {wkhtmltopdf_path, "/usr/bin/wkhtmltopdf"}

## Load testing

Download and compile basho_bench first, then

    ERL_LIBS=path_to_ehtmltopdf ./basho_bench path_to_ehtmltopdf/priv/ehtmltopdf_bb_driver.config

Note: It will copy priv/mess.svg to your /tmp to make the <img> path simple.

    make results

Enjoy the beautiful results in `current/summary.png` etc.

As a reference, on some MacBook Pro I got a consistent 3 converts per second
for 5 minutes. RAM usage looked sane, CPU usage was high. Changing the config
to have 6 concurrent workers leads to about 4 operations per second. In both
cases the Ops/sec tends slowly upwards.

## License
The KIVRA ehtmltopdf library uses an [MIT license](http://en.wikipedia.org/wiki/MIT_License). So go ahead and do what you want!
=======
ehtmltopdf
==========

Erlang wrapper for converting HTML to PDF
>>>>>>> 3c35463... Initial commit
