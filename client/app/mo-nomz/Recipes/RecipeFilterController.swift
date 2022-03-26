//
//  RecipeFilterController.swift
//  mo-nomz
//
//  Created by Dan Fithian on 3/6/22.
//

import UIKit

protocol RecipeFilterDelegate {
    func updateSelectedTag(active: Bool, tag: String?)
}

class RecipeFilterController: UICollectionViewController, UICollectionViewDelegateFlowLayout {
    var delegate: RecipeFilterDelegate? = nil
    var active: Bool = true
    var tags: [String]? = nil
    var selected: String? = nil
    var onChange: (() -> Void)? = nil
    
    let ACTIVE = 0
    let TAGS = 1

    override func numberOfSections(in collectionView: UICollectionView) -> Int {
        return 2
    }
    
    override func collectionView(_ collectionView: UICollectionView, numberOfItemsInSection section: Int) -> Int {
        switch section {
        case ACTIVE: return 1
        case TAGS: return tags?.count ?? 0
        default: return 0
        }
    }
    
    private func sendUpdates() {
        delegate?.updateSelectedTag(active: active, tag: selected)
        collectionView.reloadData()
    }
    
    func clear() {
        active = true
        selected = nil
        sendUpdates()
    }
    
    @objc func didTapActive(_ sender: Any?) {
        active = !active
        sendUpdates()
    }
    
    private func toggleTag(_ index: Int) {
        let tag_ = tags![index]
        if selected == tag_ {
            selected = nil
        } else {
            selected = tag_
        }
        sendUpdates()
    }
    
    override func collectionView(_ collectionView: UICollectionView, cellForItemAt indexPath: IndexPath) -> UICollectionViewCell {
        switch indexPath.section {
        case ACTIVE:
            let cell = collectionView.dequeueReusableCell(withReuseIdentifier: "activeButton", for: indexPath) as! OneCellButton
            cell.button.setTitle("active", for: .normal)
            cell.button.layer.cornerRadius = 5
            if active {
                cell.button.backgroundColor = UIColor.systemGray5
            } else {
                cell.button.backgroundColor = nil
            }
            cell.button.addTarget(self, action: #selector(didTapActive), for: .touchUpInside)
            return cell
        case TAGS:
            let cell = collectionView.dequeueReusableCell(withReuseIdentifier: "tagButton", for: indexPath) as! OneCellButton
            let tag_ = tags![indexPath.row]
            cell.button.tag = indexPath.row
            cell.button.setTitle(tag_, for: .normal)
            cell.button.layer.cornerRadius = 5
            if selected == tag_ {
                cell.button.backgroundColor = UIColor.systemGray5
            } else {
                cell.button.backgroundColor = nil
            }
            return cell
        default:
            return UICollectionViewCell()
        }
    }
    
    override func collectionView(_ collectionView: UICollectionView, contextMenuConfigurationForItemAt indexPath: IndexPath, point: CGPoint) -> UIContextMenuConfiguration? {
        switch indexPath.section {
        case TAGS:
            guard let tag_ = tags?[indexPath.row] else { return nil }
            return UIContextMenuConfiguration(identifier: nil, previewProvider: nil, actionProvider: { _ in
                    UIMenu(children: [
                        UIAction(title: "Edit tag", image: UIImage(systemName: "pencil"), handler: { _ in
                            self.promptGetInput(title: "Edit tag", content: tag_, configure: nil, completion: { (new) in
                                Database.updateTag(old: tag_, new: new)
                                self.onChange?()
                            })
                        }),
                        UIAction(title: "Delete tag", image: UIImage(systemName: "xmark"), attributes: .destructive, handler: { _ in
                            self.promptForConfirmation(title: "Delete tag", message: "Are you sure you want to delete?", handler: { _ in
                                Database.deleteTag(old: tag_)
                                self.onChange?()
                            })
                        })
                    ])
            })
        default: return nil
        }
    }
    
    override func collectionView(_ collectionView: UICollectionView, didSelectItemAt indexPath: IndexPath) {
        switch indexPath.section {
        case ACTIVE:
            didTapActive(nil)
            break
        case TAGS:
            toggleTag(indexPath.row)
            break
        default: break
        }
    }
    
    func collectionView(_ collectionView: UICollectionView, layout collectionViewLayout: UICollectionViewLayout, sizeForItemAt indexPath: IndexPath) -> CGSize {
        let text: String
        switch indexPath.section {
        case ACTIVE:
            text = "active"
            break
        case TAGS:
            text = tags?[indexPath.row] ?? ""
            break
        default:
            text = ""
            break
        }
        let font = UIFont.systemFont(ofSize: 12)
        let fontAttributes = [NSAttributedString.Key.font: font]
        let buttonWidth = 25.0
        let width = (text as NSString).size(withAttributes: fontAttributes as [NSAttributedString.Key : Any]).width + buttonWidth
        let height = view.frame.size.height - 10
        return CGSize(width: width, height: height)
    }
    
    private func loadData() {
        tags = Database.getTopTags()
    }
    
    func reloadData() {
        loadData()
        collectionView.reloadData()
    }
    
    override func viewDidLoad() {
        super.viewDidLoad()
        loadData()
    }
}
