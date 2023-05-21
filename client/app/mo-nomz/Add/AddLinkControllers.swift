//
//  AddLinkController.swift
//  mo-nomz
//
//  Created by Dan Fithian on 4/12/21.
//

import UIKit

class AddLinkController: AddDetailController {
    @IBOutlet weak var link: UITextField!

    var response: ParseLinkResponse? = nil
    
    @IBAction func didTapSubmit(_ sender: Any?) {
        if let text = link.text?.nonEmpty() {
            addLink(link: text, completion: { response in
                self.response = response
                DispatchQueue.main.async {
                    self.performSegue(withIdentifier: "pushManualRecipe", sender: nil)
                }
            })
        } else {
            alertUnsuccessful("Please provide a link to a recipe.")
        }
    }
    
    override func prepare(for segue: UIStoryboardSegue, sender: Any?) {
        if let vc = segue.destination as? AddManualController, segue.identifier == "pushManualRecipe" {
            vc.change = .link(link.text, response?.name, response?.ingredients.map({ $0.render() }).joined(separator: "\n") ?? "", response?.steps.joined(separator: "\n"))
        }
    }
    
    override func viewDidLoad() {
        super.viewDidLoad()
        link.addDoneButtonOnKeyboard()
    }
}
