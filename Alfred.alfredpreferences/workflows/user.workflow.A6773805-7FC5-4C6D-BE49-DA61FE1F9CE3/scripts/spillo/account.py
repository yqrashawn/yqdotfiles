import os

from Foundation import (
    NSData,
    NSLibraryDirectory,
    NSPropertyListSerialization,
    NSSearchPathForDirectoriesInDomains,
    NSUserDomainMask,
)

def retrieve_account_identifiers():
    path = NSSearchPathForDirectoriesInDomains(NSLibraryDirectory, NSUserDomainMask, True).firstObject()
    path = os.path.join(path,
                        'Group Containers',
                        'Q8B696Y8U4.com.ddeville.spillo',
                        'Library',
                        'Preferences',
                        'Q8B696Y8U4.com.ddeville.spillo.plist'
                        )

    data = NSData.dataWithContentsOfFile_(path)
    if data is None:
        return None

    defaults = NSPropertyListSerialization.propertyListWithData_options_format_error_(data, 0, None, None)[0]
    if defaults is None:
        return None

    accounts = defaults.get("accounts")
    if accounts is None:
        return None

    return accounts.valueForKey_("identifier")
