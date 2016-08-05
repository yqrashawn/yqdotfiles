# Skim Remote
## Control Skim with Alfred Remote

Out of the box, Skim Remote provides a remote to cover presentation needs in Skim.

## Roadmap

* Direct scrolling is currently unsupported in Alfred Remote, but as soon as it is, I'll add it here.
* I only created icons for the default commands, but I have the template and can create additional ones if there's demand.
* implement goto_bookmark()

## Details

The heart of the workflow is a single Applescript, SkimUI.scpt, that defines the following handlers:

### View

single_page()
single_page_continuous()
double_page()
double_page_continuous()
page_breaks_on()
page_breaks_off()
normal_mode()
full_screen_mode()
presentation_mode()
zoom_to_fit()
zoom_in()
zoom_out()
zoom_to(z) where z is the desired zoom (1 = 100%, 0.5 = 50% etc.)

### Navigation

goto_last_page()
goto_first_page()
next_page()
prev_page()

### Info

pages()
file_name()
ESC() - used for clearing large type display of pages or file name

## Customization

The Applescript to call one of these (e.g. file_name) looks like this:

set workflowFolder to do shell script "pwd"
set sk to load script POSIX file (workflowFolder & "/SkimUI.scpt")
sk's file_name()

This could easily be extended:

set workflowFolder to do shell script "pwd"
set sk to load script POSIX file (workflowFolder & "/SkimUI.scpt")
sk's full_screen_mode()
sk's zoom_to(1)
sk's single_page_continuous()
sk's goto_first_page()


